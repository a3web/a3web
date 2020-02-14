#include <stdio.h>
#include <curl/curl.h>

__attribute__((dllexport)) void RVExtension(char *output, int outputSize, const char *function);
__attribute__((dllexport)) int RVExtensionArgs(char *output, int outputSize, const char *function, const char **argv, int argc);
__attribute__((dllexport)) void RVExtensionVersion(char *output, int outputSize);

int strncpy_safe(char *output, const char *src, int size)
{
	int i;
	size--;
	for (i = 0; i < size && src[i] != '\0'; i++)
	{
		output[i] = src[i];
	}
	output[i] = '\0';
	return i;
}

void RVExtension(char *output, int outputSize, const char *function)
{
	strncpy_safe(output, function, outputSize);
	printf("RVExtention: test call\n");
}

int RVExtensionArgs(char *output, int outputSize,
		const char *function,
		const char **argv, int argc)
{
	CURL *curl;
	CURLcode res;

	/* get a curl handle */
	curl = curl_easy_init();
	if(curl) {
		/* First set the URL that is about to receive our POST. This URL can
		   just as well be a https:// URL if that is what should receive the
		   data. */
		curl_easy_setopt(curl, CURLOPT_URL, "http://localhost:5000/post-test");
		/* Now specify the POST data */
		curl_easy_setopt(curl, CURLOPT_POSTFIELDS, "Arma3 request!");

		/* Perform the request, res will get the return code */
		res = curl_easy_perform(curl);
		/* Check for errors */
		if(res != CURLE_OK)
			fprintf(stderr, "curl_easy_perform() failed: %s\n",
					curl_easy_strerror(res));

		/* always cleanup */
		curl_easy_cleanup(curl);
	}
	return 0;
}

void RVExtensionVersion(char *output, int outputSize)
{
	curl_global_init(CURL_GLOBAL_ALL);
	strncpy_safe(output, "Leo-Extension v0.1", outputSize);
}
